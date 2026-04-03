# dlaev2

> Compute the eigendecomposition of a 2-by-2 symmetric matrix.

<section class="usage">

## Usage

```javascript
var dlaev2 = require( '@stdlib/lapack/base/dlaev2' );
```

#### dlaev2( a, b, c )

Computes the eigendecomposition of a 2-by-2 symmetric matrix

```text
[ a  b ]
[ b  c ]
```

returning an object with eigenvalues `rt1` (larger absolute value) and `rt2` (smaller absolute value), and the unit right eigenvector components `cs1` and `sn1` for `rt1`.

```javascript
var out = dlaev2( 2.0, 1.0, 3.0 );
// returns { rt1: ~3.618, rt2: ~1.382, cs1: ~-0.526, sn1: ~0.851 }
```

The rotation matrix `[cs1, sn1; -sn1, cs1]` diagonalizes the input:

```text
[ cs1  sn1 ] [ a  b ] [ cs1 -sn1 ]   [ rt1  0  ]
[-sn1  cs1 ] [ b  c ] [ sn1  cs1 ] = [  0  rt2 ]
```

The function has the following parameters:

-   **a**: the (1,1) element of the 2-by-2 matrix.
-   **b**: the (1,2) element (and (2,1) element) of the 2-by-2 matrix.
-   **c**: the (2,2) element of the 2-by-2 matrix.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `rt1` is accurate to a few ulps barring over/underflow.
-   `rt2` may be inaccurate if there is massive cancellation in the determinant `a*c - b*b`.
-   `cs1` and `sn1` are accurate to a few ulps barring over/underflow.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var dlaev2 = require( '@stdlib/lapack/base/dlaev2' );

var out = dlaev2( 2.0, 1.0, 3.0 );
console.log( 'rt1:', out.rt1, 'rt2:', out.rt2 );
console.log( 'cs1:', out.cs1, 'sn1:', out.sn1 );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

</section>

<!-- /.links -->
