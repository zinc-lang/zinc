---
author: Ketan Reynolds
date: 2022/06/13
---

# Allocation errors considered harmful (to programmers)

Yes, I hate myself as mush as you do for that title.

## Problem

I believe strong default are **very** important.
I do not think forcing programmers to handle allocation errors is a strong default.
It forces code that would otherwise never need to manage those errors manage them.
This ends up creating large swaths of code that would never be erroneous in the first place.
When most programmers are not going to do anything with the fact that an allocation error has occurred, and just crash.
Resulting in zero difference in behaviour to the end user.

Allocation errors are a special case, and as such they should be specially handled.
I would still find in unacceptable if I could not handle in any way possible.
The means of doing so should just be out of stream and opt in. Unlike most errors faced.

## Solution

I'm playing with the concept of a context object. Like in Odin.
One that's changes only apply per scope. ie. at the end of a scope it will return to what it was before the beginning of the scope.
Within this context we can store the allocator.
And a part of this allocator could be a pointer to a function called on such an allocation error.

So the scenario I'm imagining in my head, we have an allocator stored in a context of which any changes are scope local.
This allocator would have a default behaviour on an error that would be okay for most cases.
But one could just replace this function with a closure to handle the allocation failure on their own terms and possibly recover.
