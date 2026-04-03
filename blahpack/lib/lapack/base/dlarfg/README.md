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

# dlarfg

> Generate a real elementary reflector (Householder matrix).

<section class="usage">

## Usage

```javascript
var dlarfg = require( '@stdlib/lapack/base/dlarfg' );
```

#### dlarfg.ndarray( N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau )

Generates a real elementary reflector H of order `N`, such that

```text
H * ( alpha ) = ( beta ),   H^T * H = I.
    (   x   )   (   0  )
```

where `alpha` and `beta` are scalars and `x` is an `(N-1)`-element real
vector. `H` is represented in the form

```text
H = I - tau * ( 1 ) * ( 1  v^T ),
              ( v )
```

where `tau` is a real scalar and `v` is a real `(N-1)`-element vector.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var alpha = new Float64Array( [ 3.0 ] );
var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );
var tau = new Float64Array( 1 );

dlarfg.ndarray( 4, alpha, 0, x, 1, 0, tau, 0 );
// alpha[0] => -5.0
// tau[0] => 1.6
// x => [ -0.8, 0.0, 0.0 ]
```

The function has the following parameters:

-   **N**: order of the elementary reflector.
-   **alpha**: on entry, the value alpha; on exit, overwritten with beta. Passed as a `Float64Array` with an offset.
-   **offsetAlpha**: index into `alpha` array.
-   **x**: input vector of length `(N-1)`. On exit, overwritten with the vector `v`.
-   **strideX**: stride length for `x`.
-   **offsetX**: starting index for `x`.
-   **tau**: output scalar. Passed as a `Float64Array` with an offset.
-   **offsetTau**: index into `tau` array.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   If the elements of `x` are all zero, then `tau = 0` and `H` is taken to be the unit matrix.
-   Otherwise `1 <= tau <= 2`.
-   `alpha` is passed as an array (not a scalar) because it is modified in place (overwritten with `beta` on exit).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlarfg = require( '@stdlib/lapack/base/dlarfg' );

var alpha = new Float64Array( [ 3.0 ] );
var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );
var tau = new Float64Array( 1 );

dlarfg.ndarray( 4, alpha, 0, x, 1, 0, tau, 0 );

console.log( 'alpha (beta):', alpha[ 0 ] );
// => alpha (beta): -5
console.log( 'tau:', tau[ 0 ] );
// => tau: 1.6
console.log( 'v:', x );
// => v: Float64Array [ -0.8, 0, 0 ]
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

</section>

<!-- /.links -->
