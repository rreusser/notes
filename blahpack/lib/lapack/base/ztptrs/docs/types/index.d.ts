

// TypeScript declarations for @stdlib/lapack/base/ztptrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a triangular system of equations with a triangular matrix in packed storage.
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
* Solves a triangular system of equations with a triangular matrix in packed storage.
*/
declare var ztptrs: Routine;

export = ztptrs;
