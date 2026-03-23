

// TypeScript declarations for @stdlib/blas/base/ztbsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a complex triangular banded system of equations
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
* Solve a complex triangular banded system of equations
*/
declare var ztbsv: Routine;

export = ztbsv;
