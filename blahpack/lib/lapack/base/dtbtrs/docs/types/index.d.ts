

// TypeScript declarations for @stdlib/lapack/base/dtbtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular banded system of equations.
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
* Solve a triangular banded system of equations.
*/
declare var dtbtrs: Routine;

export = dtbtrs;
