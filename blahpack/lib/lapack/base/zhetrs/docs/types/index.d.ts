

// TypeScript declarations for @stdlib/lapack/base/zhetrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a system of linear equations A*X = B with a Hermitian indefinite matrix using Bunch-Kaufman factorization
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve a system of linear equations A*X = B with a Hermitian indefinite matrix using Bunch-Kaufman factorization
*/
declare var zhetrs: Routine;

export = zhetrs;
