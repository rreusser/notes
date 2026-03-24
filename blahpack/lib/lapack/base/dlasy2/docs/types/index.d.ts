

// TypeScript declarations for @stdlib/lapack/base/dlasy2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices
	*/
	(
		ltranl: boolean,
		ltranr: boolean,
		isgn: number,
		n1: number,
		n2: number,
		TL: Float64Array,
		strideTL1: number,
		strideTL2: number,
		offsetTL: number,
		TR: Float64Array,
		strideTR1: number,
		strideTR2: number,
		offsetTR: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		scale: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		xnorm: number
	): Float64Array;
}

/**
* Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices
*/
declare var dlasy2: Routine;

export = dlasy2;
