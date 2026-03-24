

// TypeScript declarations for @stdlib/lapack/base/zgelss

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the minimum norm solution to a complex linear least squares problem using SVD
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
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Computes the minimum norm solution to a complex linear least squares problem using SVD
*/
declare var zgelss: Routine;

export = zgelss;
