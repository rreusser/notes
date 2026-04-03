# dlanv2

> Compute the Schur factorization of a real 2-by-2 nonsymmetric matrix in standard form.

<section class="usage">

## Usage

```javascript
var dlanv2 = require( '@stdlib/lapack/base/dlanv2' );
```

#### dlanv2( a, b, c, d )

Computes the Schur factorization of a real 2-by-2 nonsymmetric matrix in standard form:

```text
[ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
[ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
```

where either

1. `CC = 0` so that `AA` and `DD` are real eigenvalues of the matrix, or
2. `AA = DD` and `BB*CC < 0`, so that `AA +/- sqrt(BB*CC)` are complex conjugate eigenvalues.

```javascript
var result = dlanv2( 4.0, 1.0, 2.0, 3.0 );
// returns {...}
```

The function returns an object with the following fields:

-   **a**: the (1,1) element of the Schur form.
-   **b**: the (1,2) element of the Schur form.
-   **c**: the (2,1) element of the Schur form.
-   **d**: the (2,2) element of the Schur form.
-   **rt1r**: real part of the first eigenvalue.
-   **rt1i**: imaginary part of the first eigenvalue.
-   **rt2r**: real part of the second eigenvalue.
-   **rt2i**: imaginary part of the second eigenvalue.
-   **cs**: cosine of the rotation matrix.
-   **sn**: sine of the rotation matrix.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   When eigenvalues are complex conjugate pairs, `rt1i > 0`.
-   The rotation matrix `Q = [ CS, -SN; SN, CS ]` is orthogonal and satisfies `Q^T * [A B; C D] * Q = [AA BB; CC DD]`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var dlanv2 = require( '@stdlib/lapack/base/dlanv2' );

// Real eigenvalue case:
var result = dlanv2( 4.0, 1.0, 2.0, 3.0 );
console.log( result );

// Complex eigenvalue case:
result = dlanv2( 1.0, -5.0, 1.0, 1.0 );
console.log( result );
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
