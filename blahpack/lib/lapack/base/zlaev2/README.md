# zlaev2

> Computes the eigendecomposition of a 2-by-2 Hermitian matrix.

<section class="usage">

## Usage

```javascript
var zlaev2 = require( '@stdlib/lapack/base/zlaev2' );
```

#### zlaev2( a, b, c )

Computes the eigendecomposition of a 2-by-2 Hermitian matrix `[[A, B], [conj(B), C]]` where A and C are real (imaginary parts ignored) and B is complex.

```javascript
var Complex128 = require( '@stdlib/complex/float64/ctor' );

var a = new Complex128( 5.0, 0.0 );
var b = new Complex128( 1.0, 2.0 );
var c = new Complex128( 3.0, 0.0 );

var out = zlaev2( a, b, c );
// returns { rt1: ~6.449, rt2: ~1.551, cs1: ~-0.839, sn1r: ~-0.243, sn1i: ~0.487 }
```

The function has the following parameters:

-   **a**: `Complex128` - the (1,1) element of the 2-by-2 Hermitian matrix (only real part used).
-   **b**: `Complex128` - the (1,2) element of the 2-by-2 Hermitian matrix.
-   **c**: `Complex128` - the (2,2) element of the 2-by-2 Hermitian matrix (only real part used).

The function returns an object with the following fields:

-   **rt1**: eigenvalue of larger absolute value.
-   **rt2**: eigenvalue of smaller absolute value.
-   **cs1**: cosine of the rotation (real).
-   **sn1r**: real part of the sine of the rotation.
-   **sn1i**: imaginary part of the sine of the rotation.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zlaev2` is the complex analog of [`dlaev2`][@stdlib/lapack/base/dlaev2]. It handles the case where the off-diagonal element B is complex by computing a phase factor `W = conj(B)/|B|`, calling the real `dlaev2` with `(real(A), |B|, real(C))`, and rotating the sine component by W.
-   RT1 always satisfies `|RT1| >= |RT2|`.
-   The eigenvector `(CS1, SN1)` is a unit vector: `CS1^2 + |SN1|^2 = 1`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var zlaev2 = require( '@stdlib/lapack/base/zlaev2' );

var a = new Complex128( 5.0, 0.0 );
var b = new Complex128( 1.0, 2.0 );
var c = new Complex128( 3.0, 0.0 );

var out = zlaev2( a, b, c );
console.log( 'rt1:', out.rt1 );
console.log( 'rt2:', out.rt2 );
console.log( 'cs1:', out.cs1 );
console.log( 'sn1:', out.sn1r, '+', out.sn1i, 'i' );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/lapack/base/dlaev2]: https://github.com/stdlib-js/stdlib/tree/develop/lib/node_modules/%40stdlib/lapack/base/dlaev2

</section>

<!-- /.links -->
