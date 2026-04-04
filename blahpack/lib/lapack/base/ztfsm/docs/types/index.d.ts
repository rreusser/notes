

// TypeScript declarations for @stdlib/lapack/base/ztfsm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed format.
	*/
	(
		transr: string,
		side: string,
		uplo: string,
		trans: string,
		diag: string,
		M: number,
		N: number,
		alpha: any,
		a: Float64Array,
		strideA: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		ldb: number
	): Float64Array;
}

/**
* Solves a matrix equation with a complex triangular matrix in Rectangular Full Packed format.
*/
declare var ztfsm: Routine;

export = ztfsm;
