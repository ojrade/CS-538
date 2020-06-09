# Asynchronous Concurrent RSS Indexer

In this assignment, you will take an indexer for RSS feeds and make it
concurrent, much like you did in HW6. You will use Rust's futures and
async/await syntax to make an **asynchronous** indexer.

## Review: Read and run the single-threaded version (`common.rs`, `single.rs`)

The first step is to look over the non-concurrent indexer, which we've given you
split between two files. The common functionality is in `common.rs`, and the
single-threaded processing functions are in `single.rs`. Take a close look here:
we've made a few changes in behavior. Most notably, we've gotten rid of the URL
`HashSet`: for this assignment, **you should not try to detect duplicate URLs**.

## Main task: Multi-threaded version (`asynchro.rs`)

Now, you're going to make the indexer use asynchronous concurrency, by
completing `asynchro.rs`.

In `main.rs`, we've set up and called the `tokio` runtime. Note that we've
configured the runtime so that the executor uses only a single OS thread, just
like the version in `single.rs`. Once you get this version working, though, you
will see that the performance of the asynchronous version is far better.

## The `reqwest` crate

You will be working with `reqwest`, a crate for HTTP requests. This crate
includes blocking (that is, standard) and async versions of each function. To
start, note that `single.rs` uses `reqwest::blocking::get` to read from a URL.
This call blocks the current thread. In your code, you should use the async
version of this function: `reqwest::get`. This call will yield construct a
future that yields control to other tasks while the GET request is processing.

## Hints

1. We have designed this assignment so that almost all of the code in
   `single.rs` can be re-used---you should be able to tweak a few lines of code
   and have a working asynchronous indexer. Copy-pasting the code in `single.rs`
   in to the respective functions in `asynchro.rs` is a good place to start.
2. You'll need to use a bit of the Rust `futures` library. This is a large
   library with a lot of associated types, traits, and methods. For this
   assignment, you should use an unordered and buffered stream:

   https://docs.rs/futures/0.3.4/futures/stream/trait.StreamExt.html#method.buffer_unordered

   Take a look at the examples to see how this works.
3. You should set the size of the buffer to be `MAX_CONN`. In the unlikely case
   that you get errors from hitting OS limits, you can reduce the value of
   `MAX_CONN`. If you're curious, you can see what happens if you increase the
   limit too much.
4. We won't time your asynchronous indexer. But for a rough idea, you should be
   able to parse the medium feed file in a reasonable amount of time (on my
   machine, about one minute).
