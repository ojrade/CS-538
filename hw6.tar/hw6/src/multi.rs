use rss::Channel;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufReader;
use std::result::Result;

use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use url::Url;

use crate::common::*;

/// Thread limits.
const MAX_THREADS_FEEDS: u32 = 5;
const MAX_THREADS_SITES: u32 = 10;
const MAX_THREADS_TOTAL: u32 = 18;

/// A lock around some T, with a condition variable for notifying/waiting.
struct CvarLock<T> {
    mutex: Mutex<T>,
    condvar: Condvar,
}

impl<T> CvarLock<T> {
    fn new(data: T) -> Self {
        let mutex = Mutex::new(data);
        let condvar = Condvar::new();
        CvarLock { mutex, condvar }
    }
}

/// Locks/Condvars around counters, tracking the number of feed threads, the number of article
/// threads per hostname, and the total number of threads.
pub struct ThreadCount {
    feeds_count: CvarLock<u32>,
    sites_count: CvarLock<HashMap<String, u32>>,
    total_count: CvarLock<u32>,
}

/// Same as for the single-threaded version, but now spawn a new thread for each call to
/// `process_feed`. Make sure to respect the thread limits!
pub fn process_feed_file(file_name: &str, index: Arc<Mutex<ArticleIndex>>) -> RssIndexResult<()> {
    let file = File::open(file_name)?;
    println!("Processing feed file: {}", file_name);

    let channel = Channel::read_from(BufReader::new(file))?;

    let mut urls = Arc::new(Mutex::new(HashSet::new()));

    let mut pro = vec![];

    let tCount = ThreadCount{
        feeds_count:CvarLock::new(0),
        sites_count:CvarLock::new(HashMap::new()),
        total_count:CvarLock::new(0)
    };

    
    let tLock = Arc::new(tCount);

    for feed in channel.into_items() {
        let url = feed.link().ok_or(RssIndexError::UrlError)?;
        let title = feed.title().ok_or(RssIndexError::UrlError)?;
        let totC = tLock.clone();
        let urlC = urls.clone();

        let mut urlu = urlC.lock().unwrap();
        if urlu.contains(url) {
            println!("Skipping already seen feed: {} [{}]", title, url);
            continue;
        }
        urlu.insert(url.to_string());
        std::mem::drop(urlu);

        let mut feedCount = totC.feeds_count.mutex.lock().unwrap();
        while(*feedCount >= MAX_THREADS_FEEDS){
            feedCount = totC.feeds_count.condvar.wait(feedCount).unwrap();
        }
        std::mem::drop(feedCount);

        let mut totCount = totC.total_count.mutex.lock().unwrap();
        while(*totCount >= MAX_THREADS_TOTAL){
            totCount = totC.total_count.condvar.wait(totCount).unwrap();
        }
        std::mem::drop(totCount);
        let indexC = index.clone();

        println!("Processing feed: {} [{}]", title, url);
        pro.push(thread::spawn(move || {
            let thr = totC.clone();
            let f = feed;
            let u = f.link().ok_or(RssIndexError::UrlError).unwrap();
            let mut feedCount = thr.feeds_count.mutex.lock().unwrap();
            *feedCount = *feedCount + 1;
            std::mem::drop(feedCount);
            
            let mut totCount = thr.total_count.mutex.lock().unwrap();
            *totCount = *totCount + 1;
            std::mem::drop(totCount);

            process_feed(u, indexC, urlC, thr.clone()).unwrap_or_default();

            let mut feedCount = thr.feeds_count.mutex.lock().unwrap();
            *feedCount = *feedCount - 1;
            totC.feeds_count.condvar.notify_one();
            std::mem::drop(feedCount);
            
            let mut totCount = thr.total_count.mutex.lock().unwrap();
            *totCount = *totCount - 1;
            totC.total_count.condvar.notify_all();
            std::mem::drop(totCount);
        }));
    }
    for p in pro {
        p.join().unwrap();
    };
    Result::Ok(())
}

/// Same as for the single-threaded version, but now spawn a new thread for each call to
/// `process_article`. Make sure to respect the thread limits!
fn process_feed(
    url: &str,
    index: Arc<Mutex<ArticleIndex>>,
    urls: Arc<Mutex<HashSet<String>>>,
    counters: Arc<ThreadCount>,
) -> RssIndexResult<()> {
    let contents = reqwest::blocking::get(url)?.bytes()?;
    let channel = Channel::read_from(&contents[..])?;
    let items = channel.into_items();
    let mut pro = vec![];

    for item in items {
        let totC = counters.clone();
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
        let siteC = site.clone();

        let mut siteMap = totC.sites_count.mutex.lock().unwrap();
        if((*siteMap).contains_key(&site)){
            while(*(*siteMap).get_mut(&siteC).unwrap() >= MAX_THREADS_SITES){
                siteMap = totC.sites_count.condvar.wait(siteMap).unwrap();
            }
        }
        std::mem::drop(siteMap);

        
        let mut totCount = totC.total_count.mutex.lock().unwrap();
        while(*totCount >= MAX_THREADS_TOTAL){
            totCount = totC.total_count.condvar.wait(totCount).unwrap();
        }
        std::mem::drop(totCount);
        let indC = index.clone();

        pro.push(thread::spawn(move || { 
            let article = Article::new(urlC.to_string(), titleC.to_string());
            let site2 = siteC.clone();
            let mut siteMap = totC.sites_count.mutex.lock().unwrap();
            if((*siteMap).contains_key(&siteC)){
                *(*siteMap).get_mut(&siteC).unwrap() += 1;
            }
            else{
                (*siteMap).insert(site,1);
            }
            std::mem::drop(siteMap);
            
            let mut totCount = totC.total_count.mutex.lock().unwrap(); 
            *totCount = *totCount + 1;
            std::mem::drop(totCount);

            let article_words = process_article(&article).unwrap_or_default();
            
            let mut indexL = indC.lock().unwrap();
            indexL.add(
                site2.to_string(),
                titleC.to_string(),
                urlC.to_string(),
                article_words,
            );
            std::mem::drop(indexL);
            let mut siteMap = totC.sites_count.mutex.lock().unwrap();
            *(*siteMap).get_mut(&site2).unwrap() -= 1;
            totC.sites_count.condvar.notify_all();
            std::mem::drop(siteMap);

            let mut totCount = totC.total_count.mutex.lock().unwrap(); 
            *totCount = *totCount - 1;
            totC.total_count.condvar.notify_all();
            std::mem::drop(totCount);
        }));
    }
    Result::Ok(())
}
