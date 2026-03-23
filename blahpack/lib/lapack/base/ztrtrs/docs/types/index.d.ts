

// TypeScript declarations for @stdlib/lapack/base/ztrtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a complex triangular system with multiple right-hand sides
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve a complex triangular system with multiple right-hand sides
*/
declare var ztrtrs: Routine;

export = ztrtrs;
