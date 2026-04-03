

// TypeScript declarations for @stdlib/lapack/base/dsptrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a system of linear equations with a real symmetric matrix in packed storage using the factorization computed by dsptrf.
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
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
* Solves a system of linear equations with a real symmetric matrix in packed storage using the factorization computed by dsptrf.
*/
declare var dsptrs: Routine;

export = dsptrs;
