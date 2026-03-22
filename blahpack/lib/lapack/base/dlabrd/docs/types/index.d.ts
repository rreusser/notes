

// TypeScript declarations for @stdlib/lapack/base/dlabrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce the first NB rows and columns of a matrix to bidiagonal form
	*/
	(
		M: number,
		N: number,
		nb: number,
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
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		Y: Float64Array,
		strideY1: number,
		strideY2: number,
		offsetY: number
	): Float64Array;
}

/**
* Reduce the first NB rows and columns of a matrix to bidiagonal form
*/
declare var dlabrd: Routine;

export = dlabrd;
