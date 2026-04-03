

// TypeScript declarations for @stdlib/lapack/base/ztbtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a triangular system of equations with a triangular band matrix.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		kd: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solves a triangular system of equations with a triangular band matrix.
*/
declare var ztbtrs: Routine;

export = ztbtrs;
