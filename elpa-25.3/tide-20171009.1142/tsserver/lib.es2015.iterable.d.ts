/*! *****************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved. 
Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at http://www.apache.org/licenses/LICENSE-2.0  
 
THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE, 
MERCHANTABLITY OR NON-INFRINGEMENT. 
 
See the Apache Version 2.0 License for specific language governing permissions
and limitations under the License.
***************************************************************************** */



/// <reference no-default-lib="true"/>


/// <reference path="lib.es2015.symbol.d.ts" />

interface SymbolConstructor {
    /**
     * A method that returns the default iterator for an object. Called by the semantics of the
     * for-of statement.
     */
    readonly iterator: symbol;
}

interface IteratorResult<T> {
    done: boolean;
    value: T;
}

interface Iterator<T> {
    next(value?: any): IteratorResult<T>;
    return?(value?: any): IteratorResult<T>;
    throw?(e?: any): IteratorResult<T>;
}

interface Iterable<T> {
    [Symbol.iterator](): Iterator<T>;
}

interface IterableIterator<T> extends Iterator<T> {
    [Symbol.iterator](): IterableIterator<T>;
}

interface Array<T> {
    /** Iterator */
    [Symbol.iterator](): IterableIterator<T>;

    /**
     * Returns an iterable of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, T]>;

    /**
     * Returns an iterable of keys in the array
     */
    keys(): IterableIterator<number>;

    /**
     * Returns an iterable of values in the array
     */
    values(): IterableIterator<T>;
}

interface ArrayConstructor {
    /**
     * Creates an array from an iterable object.
     * @param iterable An iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from<T, U>(iterable: Iterable<T>, mapfn: (this: void, v: T, k: number) => U): Array<U>;
    from<T, U>(iterable: Iterable<T>, mapfn: (this: void, v: T, k: number) => U, thisArg: undefined): Array<U>;
    from<Z, T, U>(iterable: Iterable<T>, mapfn: (this: Z, v: T, k: number) => U, thisArg: Z): Array<U>;

    /**
     * Creates an array from an iterable object.
     * @param iterable An iterable object to convert to an array.
     */
    from<T>(iterable: Iterable<T>): Array<T>;
}

interface ReadonlyArray<T> {
    /** Iterator of values in the array. */
    [Symbol.iterator](): IterableIterator<T>;

    /**
     * Returns an iterable of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, T]>;

    /**
     * Returns an iterable of keys in the array
     */
    keys(): IterableIterator<number>;

    /**
     * Returns an iterable of values in the array
     */
    values(): IterableIterator<T>;
}

interface IArguments {
    /** Iterator */
    [Symbol.iterator](): IterableIterator<any>;
}

interface Map<K, V> {
    /** Returns an iterable of entries in the map. */
    [Symbol.iterator](): IterableIterator<[K, V]>;

    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(): IterableIterator<[K, V]>;

    /**
     * Returns an iterable of keys in the map
     */
    keys(): IterableIterator<K>;

    /**
     * Returns an iterable of values in the map
     */
    values(): IterableIterator<V>;
}

interface ReadonlyMap<K, V> {
    /** Returns an iterable of entries in the map. */
    [Symbol.iterator](): IterableIterator<[K, V]>;

    /**
     * Returns an iterable of key, value pairs for every entry in the map.
     */
    entries(): IterableIterator<[K, V]>;

    /**
     * Returns an iterable of keys in the map
     */
    keys(): IterableIterator<K>;

    /**
     * Returns an iterable of values in the map
     */
    values(): IterableIterator<V>;
}

interface MapConstructor {
    new <K, V>(iterable: Iterable<[K, V]>): Map<K, V>;
}

interface WeakMap<K extends object, V> { }

interface WeakMapConstructor {
    new <K extends object, V>(iterable: Iterable<[K, V]>): WeakMap<K, V>;
}

interface Set<T> {
    /** Iterates over values in the set. */
    [Symbol.iterator](): IterableIterator<T>;
    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(): IterableIterator<[T, T]>;
    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): IterableIterator<T>;

    /**
     * Returns an iterable of values in the set.
     */
    values(): IterableIterator<T>;
}

interface ReadonlySet<T> {
    /** Iterates over values in the set. */
    [Symbol.iterator](): IterableIterator<T>;

    /**
     * Returns an iterable of [v,v] pairs for every value `v` in the set.
     */
    entries(): IterableIterator<[T, T]>;

    /**
     * Despite its name, returns an iterable of the values in the set,
     */
    keys(): IterableIterator<T>;

    /**
     * Returns an iterable of values in the set.
     */
    values(): IterableIterator<T>;
}

interface SetConstructor {
    new <T>(iterable: Iterable<T>): Set<T>;
}

interface WeakSet<T> { }

interface WeakSetConstructor {
    new <T extends object>(iterable: Iterable<T>): WeakSet<T>;
}

interface Promise<T> { }

interface PromiseConstructor {
    /**
     * Creates a Promise that is resolved with an array of results when all of the provided Promises
     * resolve, or rejected when any Promise is rejected.
     * @param values An array of Promises.
     * @returns A new Promise.
     */
    all<TAll>(values: Iterable<TAll | PromiseLike<TAll>>): Promise<TAll[]>;

    /**
     * Creates a Promise that is resolved or rejected when any of the provided Promises are resolved
     * or rejected.
     * @param values An array of Promises.
     * @returns A new Promise.
     */
    race<T>(values: Iterable<T | PromiseLike<T>>): Promise<T>;
}

declare namespace Reflect {
    function enumerate(target: object): IterableIterator<any>;
}

interface String {
    /** Iterator */
    [Symbol.iterator](): IterableIterator<string>;
}

/**
 * A typed array of 8-bit integer values. The contents are initialized to 0. If the requested
 * number of bytes could not be allocated an exception is raised.
 */
interface Int8Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Int8ArrayConstructor {
    new (elements: Iterable<number>): Int8Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Int8Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Int8Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Int8Array;

    from(arrayLike: Iterable<number>): Int8Array;
}

/**
 * A typed array of 8-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Uint8Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Uint8ArrayConstructor {
    new (elements: Iterable<number>): Uint8Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Uint8Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Uint8Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Uint8Array;

    from(arrayLike: Iterable<number>): Uint8Array;
}

/**
 * A typed array of 8-bit unsigned integer (clamped) values. The contents are initialized to 0.
 * If the requested number of bytes could not be allocated an exception is raised.
 */
interface Uint8ClampedArray {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;

    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;

    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Uint8ClampedArrayConstructor {
    new (elements: Iterable<number>): Uint8ClampedArray;


    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Uint8ClampedArray;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Uint8ClampedArray;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Uint8ClampedArray;

    from(arrayLike: Iterable<number>): Uint8ClampedArray;
}

/**
 * A typed array of 16-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Int16Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;

    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;

    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Int16ArrayConstructor {
    new (elements: Iterable<number>): Int16Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Int16Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Int16Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Int16Array;

    from(arrayLike: Iterable<number>): Int16Array;
}

/**
 * A typed array of 16-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Uint16Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Uint16ArrayConstructor {
    new (elements: Iterable<number>): Uint16Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Uint16Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Uint16Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Uint16Array;

    from(arrayLike: Iterable<number>): Uint16Array;
}

/**
 * A typed array of 32-bit signed integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Int32Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Int32ArrayConstructor {
    new (elements: Iterable<number>): Int32Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Int32Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Int32Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Int32Array;

    from(arrayLike: Iterable<number>): Int32Array;
}

/**
 * A typed array of 32-bit unsigned integer values. The contents are initialized to 0. If the
 * requested number of bytes could not be allocated an exception is raised.
 */
interface Uint32Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Uint32ArrayConstructor {
    new (elements: Iterable<number>): Uint32Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Uint32Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Uint32Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Uint32Array;

    from(arrayLike: Iterable<number>): Uint32Array;
}

/**
 * A typed array of 32-bit float values. The contents are initialized to 0. If the requested number
 * of bytes could not be allocated an exception is raised.
 */
interface Float32Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Float32ArrayConstructor {
    new (elements: Iterable<number>): Float32Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Float32Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Float32Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Float32Array;

    from(arrayLike: Iterable<number>): Float32Array;
}

/**
 * A typed array of 64-bit float values. The contents are initialized to 0. If the requested
 * number of bytes could not be allocated an exception is raised.
 */
interface Float64Array {
    [Symbol.iterator](): IterableIterator<number>;
    /**
     * Returns an array of key, value pairs for every entry in the array
     */
    entries(): IterableIterator<[number, number]>;
    /**
     * Returns an list of keys in the array
     */
    keys(): IterableIterator<number>;
    /**
     * Returns an list of values in the array
     */
    values(): IterableIterator<number>;
}

interface Float64ArrayConstructor {
    new (elements: Iterable<number>): Float64Array;

    /**
     * Creates an array from an array-like or iterable object.
     * @param arrayLike An array-like or iterable object to convert to an array.
     * @param mapfn A mapping function to call on every element of the array.
     * @param thisArg Value of 'this' used to invoke the mapfn.
     */
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number): Float64Array;
    from(arrayLike: Iterable<number>, mapfn: (this: void, v: number, k: number) => number, thisArg: undefined): Float64Array;
    from<Z>(arrayLike: Iterable<number>, mapfn: (this: Z, v: number, k: number) => number, thisArg: Z): Float64Array;

    from(arrayLike: Iterable<number>): Float64Array;
}
