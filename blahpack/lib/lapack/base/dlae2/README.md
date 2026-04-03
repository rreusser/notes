# dlae2

> Compute the eigenvalues of a 2-by-2 symmetric matrix.

<section class="usage">

## Usage

```javascript
var dlae2 = require( '@stdlib/lapack/base/dlae2' );
```

#### dlae2( a, b, c )

Computes the eigenvalues of a 2-by-2 symmetric matrix

```text
[ a  b ]
[ b  c ]
```

where `rt1` is the eigenvalue of larger absolute value, and `rt2` is the eigenvalue of smaller absolute value.

```javascript
var out = dlae2( 1.0, 2.0, 1.0 );
// returns { 'rt1': 3.0, 'rt2': -1.0 }
```

The function has the following parameters:

-   **a**: the (1,1) element of the 2-by-2 matrix.
-   **b**: the (1,2) and (2,1) element of the 2-by-2 matrix.
-   **c**: the (2,2) element of the 2-by-2 matrix.

The function returns an object with the following properties:

-   **rt1**: eigenvalue of larger absolute value.
-   **rt2**: eigenvalue of smaller absolute value.

#### dlae2.ndarray( a, b, c )

Computes the eigenvalues of a 2-by-2 symmetric matrix (same as `dlae2`; provided for API consistency).

```javascript
var out = dlae2.ndarray( 1.0, 2.0, 1.0 );
// returns { 'rt1': 3.0, 'rt2': -1.0 }
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlae2` computes eigenvalues only (no eigenvectors). For eigenvalues _and_ eigenvectors of a 2-by-2 symmetric matrix, use [`dlaev2`][@stdlib/lapack/base/dlaev2].
-   `rt1` is accurate to a few ulps barring over/underflow. `rt2` may be inaccurate if there is massive cancellation in the determinant `a*c - b*b`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var dlae2 = require( '@stdlib/lapack/base/dlae2' );

// Identity matrix eigenvalues (both 1.0):
var out = dlae2( 1.0, 0.0, 1.0 );
console.log( out );
// => { 'rt1': 1.0, 'rt2': 1.0 }

// Symmetric matrix with off-diagonal:
out = dlae2( 2.0, 1.0, 2.0 );
console.log( out );
// => { 'rt1': 3.0, 'rt2': 1.0 }
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
