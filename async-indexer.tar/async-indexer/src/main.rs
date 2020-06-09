#![allow(dead_code)]
#![forbid(unsafe_code)]

use common::*;
use std::env;
use std::io;
use tokio::runtime::Builder;

mod asynchro;
mod common;
mod single;

const MAX_MATCHES: usize = 10;

fn main() -> RssIndexResult<()> {
    let mut args = env::args().skip(1);
    let mut rss_index = RssIndex::new();

    match (args.next(), args.next().as_deref()) {
        (Some(filename), Some("single")) => {
            let mut article_index = single::process_feed_file(&filename)?;
            build_index(&mut article_index, &mut rss_index);
        }
        (Some(filename), Some("async")) => {
            let mut rt = Builder::new()
                .basic_scheduler() // for multithreaded: .threaded_scheduler()
                .enable_all()
                .build()?;

            let mut article_index =
                rt.block_on(async { asynchro::process_feed_file(&filename).await })?;
            build_index(&mut article_index, &mut rss_index);
        }
        _ => {
            println!("Usage: cargo run <filename.xml> [single|async]");
            return Result::Err(Box::new(RssIndexError::ArgsError));
        }
    }

    println!("Done building index.");

    let mut buffer = String::new();
    let mut lower_buffer;

    loop {
        println!("Enter a search term [or just hit <enter> to quit]: ");
        buffer.clear();
        io::stdin().read_line(&mut buffer)?;
        buffer = buffer.trim().to_string();
        if buffer.is_empty() {
            return Result::Ok(());
        }
        lower_buffer = buffer.to_lowercase();
        let matches = rss_index.index.get(&lower_buffer);
        match matches {
            None => println!("We didn't find any matches for \"{}\". Try again.", buffer),
            Some(m) => {
                println!("That term appears in {} articles.", m.len());
                let mut articles = m.iter().collect::<Vec<(&Article, &u32)>>();
                articles.sort_by(|(art1, ct1), (art2, ct2)| {
                    // sort by decreasing hits and alphabetical title
                    ct2.cmp(&ct1).then(art1.cmp_title(&art2))
                });
                if articles.len() > MAX_MATCHES {
                    println!("Here are the top {} of them:", MAX_MATCHES);
                } else {
                    println!("Here they are:");
                }
                for (art, count) in articles
                    .iter()
                    .take(std::cmp::min(articles.len(), MAX_MATCHES))
                {
                    let times = if **count == 1 { "time" } else { "times" };
                    println!("\"{}\" [appears {} {}].", art.title, count, times);
                    println!("        \"{}\"", art.url);
                }
            }
        }
    }
}
