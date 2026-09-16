# Zinc programming language

Zinc is a programming language inspired by Rust, prioritizes simplicity over performance, with memory-safety and thread-safety fully preserved.

It is simpler than Rust by **removing ownership types and borrow checker**, trading off performance optimization. Meanwhile, Zinc also conforms "aliasing xor mutation" rule.

## Notable samples

### 1. use RC to manage all heap memories

RC is very good at tracking aliasing, since it inherently keeps a record of aliasing count. 

Zinc uses "Automatic Reference Counting", which means the compiler automatically choose atomic or non-atomic RC pointer according to pointee type. It is the most commonly used pointer type in Zinc language. So the simplest syntax is chosen for this type: `*T`.

```rust
fn main() {
    // `box` means memory allocation on heap, and it always returns ARC pointer
    let p1: *String = box String::new(); 

    // p2 and p1 are pointing to the same String
    let p2: *String = p1; 

    // ARC type `*T` can get `&T` or `&mut T` borrow types, regardless the variable is mut or not
    p1.push_str("hello");

    // modifying `p1` will affect `p2`, because they are pointing to the same object
    println(*p2);

    // when `p1` and `p2` are both destroyed, the reference count decreased to 0. And then the String is dropped.
}
```

In Zinc, **automatic reference counting** means, compiler chooses atomic or non-atomic ref-counting automatically.

Thanks to thread-safety feature, the compiler knows whether a pointer can be transferred across thread boundaries, so it knows whether atomic ref-counting is needed.
If an ARC pointer is pointing to non-`Send` or non-`Sync` type, the pointer and all its aliases can not across thread boundaries.Based on this information, compiler can use non-atomic inc/dec instructions for reference counting in this case.

### 2. move semantics exists in expressions instead of types

All types are copyable, no move-only types.

ARC pointer will automatically increase its reference count when copying.

When a user-defined type is copied, a compiler-generated copy function is called, which will automatically increase its ARC typed members' reference counts.

```rust
struct S {
    p: *Int
}

fn main() {
    // the syntax of struct initializer is the same with C language
    let s1: S = { .p = box 42_i }:S;

    // copy s1 to s2, the `p` fields will point to the same `Int`
    // and it's reference count is increased to 2
    let s2 = s1;

    // after s1 and s2 are both dropped, the heap allocated `Int` will be freed.
}
```

However, Zinc has "move semantics" in expressions, backed by control flow analysis.

```rust
fn main() {
    // the heap allocated object's reference count is 1
    let p1: *Int = box 42_i;

    // the reference count remains unchanged, because only p2 is active
    let p2 = move p1;

    *p1; // compile error, p1 can not used after move

    // after p2 is dropped, the heap allocated `Int` will be freed.
}
```

### 3. borrow to heap objects should change the ref-count of the heap object

Zinc retains borrow pointers, whose lifetime is checked at compile time.

Borrow pointers include both mutable (`&mut T`) and immutable (`&T`) borrows.

Zinc removes borrow checker, mutable borrow can co-exist with other borrows to the same object. And it can also guarantee memory safety.

```rust
fn main() {
    let mut p1: *UInt = box 1_u;
    let b1 = &*p1;  // b1 is pointing to a heap object, the ref-count of the object increases to 2.
    println(*b1);   // prints 1

    p1 = box 2_u;  // p1 is pointing to a new object, the ref-count of the old object decreases to 1.
    println(*b1);  // prints 1, because the old object is not freed. The borrow continues to be valid.
    println(*p1);  // prints 2, because p1 is pointing to the new object.
}
```

This means borrow pointer will never become a dangling pointer. A heap object will not be freed when there is an aliasing pointing to it.


### 4. use copy-on-write to implement value type collections

Copy-on-write is the key technique to achieve "aliasing xor mutation". 

Take `String` type as an example. It is possible to implement a memory-safe and thread-safe `String` type without ownership and borrow-checker. It is implemented like this:
1. internal byte buffer is managed by ARC pointer
2. methods with `&mut self` receiver always check the ref-count before modification

We can ensure when mutation happens, there is always only one aliasing pointing to the byte buffer. It conforms "aliasing xor mutation" rule perfectly.

### 5. avoid invalid iteration

Without borrow checker, we can still avoid iterator invalidation issues during iteration.

```rust
fn main() {
    let mut vec = (type Vec<Int>)::from(&[1_i, 2_i, 3_i] as Slice<Int>);

    // The iterator increased the ref-count of underlying array buffer
    for i in vec {
        // when calling mutable methods of vec, it will copy its underlying array buffer to a new allocation, and then modify.
        // this behavior is called copy-on-write.
        // the old array buffer is still alive until the iterator is dropped.
        vec.push(4);
        println(i);
    }

    // After above for loop, the vec's value is updated to [1,2,3,4,4,4].
}
```

### 6. thread safety

Similar with Rust, thread-safety is maintained by `Send` & `Sync` traits in Zinc.

```rust
struct S {}
impl !Send for S {}
static G: S = {...}:S; // error. `S` is not Send type


// ok. `&AtomicUInt` is Send type
static M: &'static AtomicUInt = &AtomicUInt::new(0);
```

Note: there is a difference between Rust and Zinc that, Rust requires `Sync` constraints for globals, whereas Zinc requires `Send` constraints for globals.

Below types conform `Send` trait, and can be safely transferred across threads:

1. "value semantic" types
2. "reference semantic" types which points to synchronized types

```rust
fn send<T>(x: T) where T: Send {}

fn main() {
    let mut vec1: Vec<Int> = Vec::new();
    let mut vec2: Vec<*Int> = Vec::new();

    send(move vec1); // ok. `Vec<Int>` is Send type.
    send(move vec2); // error. `Vec<*Int>` is not Send type
}
```

### 7. by default, generics is not implemented by monomorphization

Zinc aims to allow programmers deliver shared libraries (*.so) with public generic APIs.
Generic functions are implemented (by default) based on dictionary passing similar with Swift.

```rust
// This function can be compiled to binary code without instantiation.
pub fn generic_f<T>(arg: T) where T: MyTrait {
    arg.method(); // The method call in generic function will be dynamically dispatched
}
```

Monomorphization is only an internal optimization for some cases.

## build the compiler

Note: Only Linux/x64 platform is supported.

1. download zinc compiler from https://github.com/zinc-lang/zinc/releases , and put it in `./out/stage0` folder
2. run `python x.py build-llvm`. After this step, there will be a `./out/llvm` folder
3. create a soft link to llvm folder inside stage0
    ```
    ./out
    ├── stage0
    │   ├── bin
    │   │   └── zinc
    │   ├── lib
    │   │   ├── libstd.a
    │   │   └── std.zno
    │   └── llvm -> ../llvm
    ```
4. run `python x.py build`


## tests

```
python x.py test
```

Requires `./out/stage1/bin/zinc` from `python x.py build`. See `tests/` and [CONTRIBUTING.md](CONTRIBUTING.md).

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md). Design discussion belongs in [zinc-lang/zinc-design](https://github.com/zinc-lang/zinc-design).
