

// TypeScript declarations for @stdlib/lapack/base/dpstf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the Cholesky factorization with complete pivoting of a real symmetric positive semi-definite matrix (unblocked algorithm).
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		piv: Int32Array,
		stridePIV: number,
		offsetPIV: number,
		rank: number,
		tol: number,
		work: number
	): Float64Array;
}

/**
* Computes the Cholesky factorization with complete pivoting of a real symmetric positive semi-definite matrix (unblocked algorithm).
*/
declare var dpstf2: Routine;

export = dpstf2;
