

// TypeScript declarations for @stdlib/blas/base/dtbsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular banded system of equations
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		K: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		x: Float64Array,
		strideX: number,
		offsetX: number
	): Float64Array;
}

/**
* Solve a triangular banded system of equations
*/
declare var dtbsv: Routine;

export = dtbsv;
