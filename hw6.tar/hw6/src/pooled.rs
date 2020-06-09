use rss::Channel;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::result::Result;

use std::sync::{Arc, Mutex};
use url::Url;

use crate::common::*;
use crate::threadpool::*;

/// Thread pool sizes.
const SIZE_FEEDS_POOL: usize = 3;
const SIZE_SITES_POOL: usize = 20;

/// Same as the single/multi threaded version, but using a thread pool. Set up two thread pools:
/// one for handling feeds, and one for handling articles. Use the sizes above. Push closures
/// executing `process_feed` into the thread pool.
pub fn process_feed_file(file_name: &str, index: Arc<Mutex<ArticleIndex>>) -> RssIndexResult<()> {
    let file = File::open(file_name)?;
    println!("Processing feed file: {}", file_name);

    let channel = Channel::read_from(BufReader::new(file))?;

    let mut urls = Arc::new(Mutex::new(HashSet::new()));
    let mut fpool = ThreadPool::new(SIZE_FEEDS_POOL);
    let spool = ThreadPool::new(SIZE_SITES_POOL);
    let sLock = Arc::new(Mutex::new(spool));

    for feed in channel.into_items() {
        let url = feed.link().ok_or(RssIndexError::UrlError)?;
        let title = feed.title().ok_or(RssIndexError::UrlError)?;
        let urlC = urls.clone();
        let sClone = sLock.clone();
        let u = url.to_string();

        let mut urlu = urlC.lock().unwrap();
        if urlu.contains(url) {
            println!("Skipping already seen feed: {} [{}]", title, url);
            continue;
        }
        urlu.insert(url.to_string());
        std::mem::drop(urlu);

        let indexC = index.clone();

        println!("Processing feed: {} [{}]", title, url);

        fpool.execute(move || {
            process_feed(&u, indexC, urlC, sClone).unwrap();
        });
    }
    Result::Ok(())
}

/// Same as the single/multi threaded version, but using a thread pool. Push closures executing
/// `process_article` into the thread pool that is passed in.
fn process_feed(
    url: &str,
    index: Arc<Mutex<ArticleIndex>>,
    urls: Arc<Mutex<HashSet<String>>>,
    sites_pool: Arc<Mutex<ThreadPool>>,
) -> RssIndexResult<()> {
    let contents = reqwest::blocking::get(url)?.bytes()?;
    let channel = Channel::read_from(&contents[..])?;
    let items = channel.into_items();

    for item in items {
        let urlsC = urls.clone();
        let mut urlsL = urlsC.lock().unwrap();
        let (url, site, title) = match (item.link(), Url::parse(&url)?.host_str(), item.title()) {
            (Some(u), Some(s), Some(t)) => (u, s.to_string(), t),
            _ => continue,
        };

        if urlsL.contains(url) {
            println!("Skipping already seen article: {} [{}]", title, url);
            continue;
        }
        urlsL.insert(url.to_string());
        std::mem::drop(urlsL);

        println!("Processing article: {} [{}]", title, url);
        let urlC = url.to_string();
        let titleC = title.to_string();
        let siteC = site.to_string();

        let indC = index.clone();

        let mut spool = sites_pool.lock().unwrap();

        spool.execute(move || {
            let article = Article::new(urlC.clone(), titleC.clone());
            let article_words = process_article(&article).unwrap();
            
            let mut indexL = indC.lock().unwrap();
            indexL.add(
                siteC.to_string(),
                titleC.to_string(),
                urlC.to_string(),
                article_words,
            );
            std::mem::drop(indexL);
        })
    }
    Result::Ok(())
}
