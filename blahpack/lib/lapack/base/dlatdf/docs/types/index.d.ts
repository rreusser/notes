

// TypeScript declarations for @stdlib/lapack/base/dlatdf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes contribution to reciprocal DIF estimate using LU factorization from dgetc2
	*/
	(
		ijob: number,
		N: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		RHS: Float64Array,
		strideRHS: number,
		offsetRHS: number,
		rdsum: number,
		rdscal: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		JPIV: Int32Array,
		strideJPIV: number,
		offsetJPIV: number
	): Float64Array;
}

/**
* Computes contribution to reciprocal DIF estimate using LU factorization from dgetc2
*/
declare var dlatdf: Routine;

export = dlatdf;
