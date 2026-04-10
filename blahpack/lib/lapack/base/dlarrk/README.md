<!--

@license Apache-2.0

Copyright (c) 2025 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->

# dlarrk

> Computes one eigenvalue of a symmetric tridiagonal matrix to suitable accuracy

<section class="usage">

## Usage

```javascript
var dlarrk = require( '@stdlib/lapack/base/dlarrk' );
```

#### dlarrk( N, iw, gl, gu, d, strideD, E2, strideE2, pivmin, reltol, w, werr )

Computes one eigenvalue of a symmetric tridiagonal matrix to suitable accuracy

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **N**: number of columns.
-   **iw**: iw.
-   **gl**: gl.
-   **gu**: gu.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **E2**: output array.
-   **strideE2**: stride length for `E2`.
-   **pivmin**: pivmin.
-   **reltol**: reltol.
-   **w**: w.
-   **werr**: werr.

#### dlarrk.ndarray( N, iw, gl, gu, d, strideD, offsetD, E2, strideE2, offsetE2, pivmin, reltol, w, werr )

Computes one eigenvalue of a symmetric tridiagonal matrix to suitable accuracy, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **N**: number of columns.
-   **iw**: iw.
-   **gl**: gl.
-   **gu**: gu.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.
-   **E2**: output array.
-   **strideE2**: stride of dimension 2 of `E`.
-   **offsetE2**: starting index for `E2`.
-   **pivmin**: pivmin.
-   **reltol**: reltol.
-   **w**: w.
-   **werr**: werr.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   TODO: Add notes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
// TODO: Add examples
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
