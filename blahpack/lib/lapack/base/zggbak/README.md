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

# zggbak

> Back-transform eigenvectors of a balanced pair of matrices.

<section class="usage">

## Usage

```javascript
var zggbak = require( '@stdlib/lapack/base/zggbak' );
```

#### zggbak.ndarray( job, side, N, ilo, ihi, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, M, V, strideV1, strideV2, offsetV )

Back-transform eigenvectors of a balanced pair of matrices.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: specifies the operation type.
-   **side**: specifies the operation type.
-   **N**: number of columns.
-   **ilo**: ilo.
-   **ihi**: ihi.
-   **LSCALE**: input array.
-   **strideLSCALE**: stride length for `LSCALE`.
-   **offsetLSCALE**: starting index for `LSCALE`.
-   **RSCALE**: input array.
-   **strideRSCALE**: stride length for `RSCALE`.
-   **offsetRSCALE**: starting index for `RSCALE`.
-   **M**: number of rows.
-   **V**: output matrix.
-   **strideV1**: stride of the first dimension of `V`.
-   **strideV2**: stride of the second dimension of `V`.
-   **offsetV**: starting index for `V`.

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
