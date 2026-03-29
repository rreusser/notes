

// TypeScript declarations for @stdlib/lapack/base/dtptrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular system of equations with a triangular matrix stored in packed format.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		nrhs: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve a triangular system of equations with a triangular matrix stored in packed format.
*/
declare var dtptrs: Routine;

export = dtptrs;
