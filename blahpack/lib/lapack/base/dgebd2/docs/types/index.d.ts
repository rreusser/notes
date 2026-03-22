

// TypeScript declarations for @stdlib/lapack/base/dgebd2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce a general matrix to bidiagonal form (unblocked)
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		TAUQ: Float64Array,
		strideTAUQ: number,
		offsetTAUQ: number,
		TAUP: Float64Array,
		strideTAUP: number,
		offsetTAUP: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Reduce a general matrix to bidiagonal form (unblocked)
*/
declare var dgebd2: Routine;

export = dgebd2;
