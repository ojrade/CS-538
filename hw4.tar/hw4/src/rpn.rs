/*
 * Reverse Polish Notation: rpn.rs
 * See `rpn.md` for the overview.
 */

use std::io;
use rand::{Rng, thread_rng};
use rand::distributions::Uniform;

// Stacks will work with Items, which either either integers or booleans
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Item {
    Int(i32),
    Bool(bool),
}

// List of possible errors
#[derive(Debug)]
pub enum Error {
    Empty,         // Tried to pop empty stack
    Extra,         // Stack ended with extra elements
    Type,          // Type mismatch
    Syntax,        // Syntax error, didn't recognize op
    IO(io::Error), // Some kind of IO error
    Quit,          // User quitting
}

// Base operations supported by calculator, see rpn.md
#[derive(Debug)]
pub enum Op {
    Add,
    Eq,
    Neg,
    Swap,
    Rand,
    Cond,
    Quit,
}

// We'll define a result type for our calculator: either a valid value, or a calculator Error
pub type Result<T> = std::result::Result<T, Error>;

// Define a type for Stacks
#[derive(Debug)]
pub struct Stack(Vec<Item>);

// Implement the following functions on Stacks
impl Stack {
    // Make a new Stack
    pub fn new() -> Self {
        let items = Stack(Vec::new());
        return items;
    }

    // Check if a Stack is empty
    pub fn empty(&self) -> bool {
        return self.0.is_empty();
    }

    // Push an item onto a stack (should never error)
    pub fn push(&mut self, item: Item) -> Result<()> {
        self.0.push(item);
        return Ok(());
    }

    // Pop an item off the Stack; may result in Empty error
    pub fn pop(&mut self) -> Result<Item> {
        let popped = self.0.pop();
        match popped {
            None => return Err(Error::Empty),
            Some(x) => return Ok(x);
        }
    }

    /*
     * Main evaluation function: apply an operation to a Stack
     *
     * Complete each of the cases. 
     * Ext: make sure to push onto stack
     *
     * Hint: You'll probably want to use the "question-mark" syntax quite a bit; see `rpn.md`.
     */
    pub fn eval(&mut self, op: Op) -> Result<()> {
        match op {
            Op::Add => {
                let p1 = self.pop()?;
                match p1 {
                    Item::Bool(bool) => return Err(Error::Type),
                    Item::Int(x) => {
                        let p2 = self.pop()?;
                        match p2 {
                            Item::Bool(bool) => return Err(Error::Type),
                            Item::Int(y) => {
                                let res = x+y;
                                self.push(Item::Int(res));
                                return Ok(());
                            }

                        }
                    };
                };
            }
            Op::Eq => {
                let p1 = self.pop()?;
                match p1 {
                    Item::Bool(x) => {
                        let p2 = self.pop()?;
                        match p2 {
                            Item::Bool(y) => {
                                let res = x==y;
                                self.push(Item::Bool(res));
                                return Ok(());
                            }
                            Item::Int(i32) => return Err(Error::Type),
                        };
                    }
                    Item::Int(x) => {
                        let p2 = self.pop()?;
                        match p2 {
                            Item::Bool(bool) => return Err(Error::Type),
                            Item::Int(y) => {
                                let res = x==y;
                                self.push(Item::Bool(res));
                                return Ok(());
                            }

                        };
                    }
                };
            }
            Op::Neg => {
                let p1 = self.pop()?;
                match p1 {
                    Item::Int(i32) => return Err(Error::Type),
                    Item::Bool(x) => {
                        let res = !x;
                        self.push(Item::Bool(res));
                        return Ok(());
                    }
                };
            }
            Op::Swap => {
                let p1 = self.pop()?;
                let p2 = self.pop()?;
                self.push(p1);
                self.push(p2);
                return Ok(());
            }
            Op::Rand => {
                let p1 = self.pop()?;
                match p1 {
                    Item::Bool(bool) => return Err(Error::Type),
                    Item::Int(x) => {
                        let mut rng = thread_rng();
                        let side = Uniform::new(0,x);
                        let res = rng.sample(side);
                        self.push(Item::Int(res));
                        return Ok(());
                    }
                }
            }
            Op::Cond => {
                let o1 = self.pop()?;
                let o2 = self.pop()?;
                let p1 = self.pop()?;
                match p1 {
                    Item::Int(i32) => return Err(Error::Type),
                    Item::Bool(x) => {
                        if x {
                            self.push(o2);
                        }                 
                        else {
                            self.push(o1);
                        }                 
                        return Ok(());                                                             
                    }
                }
            }
            Op::Quit => {
                return Err(Error::Quit);
            }
            _ => return Err(Error::Syntax)
        }
        /*
        if(op==Add){
            let mut p1 = self.pop();
            let mut p2 = self.pop();
            let mut res = p1+p2;
            self.push(res)
        }
        else if (op==Eq){
            let mut e1 = self.pop();
            let mut e2 = self.pop();
            let mut res = (e1==e2)
            self.push(res)
        }
        else if (op==Neg){
            let mut n1 = self.pop();
            let mut res = !n1;
            self.push(res)
        } 
        else if (op==Swap){
            let mut s1 = self.pop();
            let mut s2 = self.pop();
            self.push(s2);
            self.push(s1);
        }
        else if (op=Rand){
            let mut r1 = self.pop();

        }*/
    }
}
