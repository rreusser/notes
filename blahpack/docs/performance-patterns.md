# Performance Patterns (from stdlib comparison)

## 1. Incremental matrix pointer

Avoid recomputing offsets in inner loops:
```javascript
// GOOD: incremental (one add per element)
var ia = offsetA + sa1 * j;
for ( i = 0; i < M; i++ ) {
    A[ ia ] = ...;
    ia += sa0;
}

// AVOID: full recompute (two multiplies per element)
for ( i = 0; i < M; i++ ) {
    A[ offsetA + i*sa1 + j*sa2 ] = ...;
}
```

## 2. Layout-aware triangle dispatch

For routines with `uplo` parameter, stdlib remaps strides so the inner
loop always walks contiguous memory:
```javascript
var isrm = isRowMajor( [ strideA1, strideA2 ] );
// (col-major, upper) ↔ (row-major, lower) are same physical pattern
var sa0 = isrm ? strideA2 : strideA1;  // fast (inner) stride
var sa1 = isrm ? strideA1 : strideA2;  // slow (outer) stride
```

## 3. Drop Fortran stride-1 specialization

The general stride/offset loop handles unit stride fine. Don't port the
`IF (INCX.EQ.1)` branches.

## 4. Preserve zero-element guards

`if (x[jx] !== 0.0)` before column updates is a meaningful optimization
(skips entire column when x is zero).
