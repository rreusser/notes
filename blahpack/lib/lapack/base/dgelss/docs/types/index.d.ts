

// TypeScript declarations for @stdlib/lapack/base/dgelss

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the minimum norm solution using SVD
	*/
	(
		M: number,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		rcond: number,
		rank: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Compute the minimum norm solution using SVD
*/
declare var dgelss: Routine;

export = dgelss;
