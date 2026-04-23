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

# dlarrd

> Computes eigenvalues of a symmetric tridiagonal matrix to suitable accuracy

<section class="usage">

## Usage

```javascript
var dlarrd = require( '@stdlib/lapack/base/dlarrd' );
```

#### dlarrd( range, order, N, vl, vu, il, iu, GERS, strideGERS, reltol, d, strideD, e, strideE, E2, strideE2, pivmin, nsplit, ISPLIT, strideISPLIT, offsetISPLIT, M, w, strideW, WERR, strideWERR, wl, wu, IBLOCK, strideIBLOCK, offsetIBLOCK, INDEXW, strideINDEXW, offsetINDEXW, WORK, strideWORK, IWORK, strideIWORK, offsetIWORK )

Computes eigenvalues of a symmetric tridiagonal matrix to suitable accuracy

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **range**: specifies the operation type.
-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **N**: number of columns.
-   **vl**: vl.
-   **vu**: vu.
-   **il**: il.
-   **iu**: iu.
-   **GERS**: input array.
-   **strideGERS**: stride length for `GERS`.
-   **reltol**: reltol.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **E2**: input array.
-   **strideE2**: stride length for `E2`.
-   **pivmin**: pivmin.
-   **nsplit**: nsplit.
-   **ISPLIT**: input array.
-   **strideISPLIT**: stride length for `ISPLIT`.
-   **offsetISPLIT**: starting index for `ISPLIT`.
-   **M**: number of rows.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **WERR**: input array.
-   **strideWERR**: stride length for `WERR`.
-   **wl**: wl.
-   **wu**: wu.
-   **IBLOCK**: input array.
-   **strideIBLOCK**: stride length for `IBLOCK`.
-   **offsetIBLOCK**: starting index for `IBLOCK`.
-   **INDEXW**: input array.
-   **strideINDEXW**: stride length for `INDEXW`.
-   **offsetINDEXW**: starting index for `INDEXW`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

#### dlarrd.ndarray( range, order, N, vl, vu, il, iu, GERS, strideGERS, offsetGERS, reltol, d, strideD, offsetD, e, strideE, offsetE, E2, strideE2, offsetE2, pivmin, nsplit, ISPLIT, strideISPLIT, offsetISPLIT, M, w, strideW, offsetW, WERR, strideWERR, offsetWERR, wl, wu, IBLOCK, strideIBLOCK, offsetIBLOCK, INDEXW, strideINDEXW, offsetINDEXW, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Computes eigenvalues of a symmetric tridiagonal matrix to suitable accuracy, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **range**: specifies the operation type.
-   **order**: specifies the operation type.
-   **N**: number of columns.
-   **vl**: vl.
-   **vu**: vu.
-   **il**: il.
-   **iu**: iu.
-   **GERS**: input array.
-   **strideGERS**: stride length for `GERS`.
-   **offsetGERS**: starting index for `GERS`.
-   **reltol**: reltol.
-   **d**: input array.
-   **strideD**: stride length for `d`.
-   **offsetD**: starting index for `D`.
-   **e**: input array.
-   **strideE**: stride length for `e`.
-   **offsetE**: starting index for `E`.
-   **E2**: input array.
-   **strideE2**: stride of dimension 2 of `E`.
-   **offsetE2**: starting index for `E2`.
-   **pivmin**: pivmin.
-   **nsplit**: nsplit.
-   **ISPLIT**: input array.
-   **strideISPLIT**: stride length for `ISPLIT`.
-   **offsetISPLIT**: starting index for `ISPLIT`.
-   **M**: number of rows.
-   **w**: input array.
-   **strideW**: stride length for `w`.
-   **offsetW**: starting index for `W`.
-   **WERR**: input array.
-   **strideWERR**: stride length for `WERR`.
-   **offsetWERR**: starting index for `WERR`.
-   **wl**: wl.
-   **wu**: wu.
-   **IBLOCK**: input array.
-   **strideIBLOCK**: stride length for `IBLOCK`.
-   **offsetIBLOCK**: starting index for `IBLOCK`.
-   **INDEXW**: input array.
-   **strideINDEXW**: stride length for `INDEXW`.
-   **offsetINDEXW**: starting index for `INDEXW`.
-   **WORK**: input array.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: output array.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

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
