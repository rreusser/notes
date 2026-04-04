

// TypeScript declarations for @stdlib/lapack/base/dsytri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a real symmetric matrix using the factorization computed by dsytrf.
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Computes the inverse of a real symmetric matrix using the factorization computed by dsytrf.
*/
declare var dsytri: Routine;

export = dsytri;
