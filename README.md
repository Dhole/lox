# Lox interpreter implemented in Zig

**Work In Progress**

This is the implementation of the Lox interpreter explained in this book:
http://craftinginterpreters.com/contents.html but in Zig.

I'm writing it while following the book.

## Status

- Part2 (A Tree-Walk Interpreter) is complete!
    - NOTE: With the introduction of closures I implemented a reference
      counting system to free the closures and the variables in those closures
      when appropiate.  With the introduction of classes, I should have
      implemented a reference counting system of that as well to free variables
      at the right moment, but I had already found it cumbersome for closures,
      and I believed there were edge cases in the closures implementation that
      would give memory leaks.  I believe the proper way to free resources in a
      dynamic language is by using a garbage collector.  Part 2 in the book is
      implemented with Java, which already has a garbage collector.  And part 3
      is implemented in C and a garbage collector is implemented as well.
      Since I did part2 in Zig, which doesn't have a garbage collector, I'm OK
      with having memory leaks (which can be fixed by implementing a garbage
      collector explained in part 3).
