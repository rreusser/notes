

// TypeScript declarations for @stdlib/lapack/base/zhfrk

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.
	*/
	(
		transr: string,
		uplo: string,
		trans: string,
		N: number,
		K: number,
		alpha: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		beta: number,
		c: Float64Array,
		strideC: number,
		offsetC: number
	): Float64Array;
}

/**
* Performs a Hermitian rank-k operation for a matrix in Rectangular Full Packed format.
*/
declare var zhfrk: Routine;

export = zhfrk;
