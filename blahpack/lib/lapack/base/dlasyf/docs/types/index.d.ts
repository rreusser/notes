

// TypeScript declarations for @stdlib/lapack/base/dlasyf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute a partial factorization of a symmetric matrix using Bunch-Kaufman pivoting
	*/
	(
		uplo: string,
		N: number,
		nb: number,
		kb: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		W: Float64Array,
		strideW1: number,
		strideW2: number,
		offsetW: number
	): Float64Array;
}

/**
* Compute a partial factorization of a symmetric matrix using Bunch-Kaufman pivoting
*/
declare var dlasyf: Routine;

export = dlasyf;
