

// TypeScript declarations for @stdlib/lapack/base/dlaln2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a 1x1 or 2x2 linear system with scaling to prevent overflow
	*/
	(
		ltrans: boolean,
		na: number,
		nw: number,
		smin: number,
		ca: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		d1: number,
		d2: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		wr: number,
		wi: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number,
		scale: number,
		xnorm: number
	): Float64Array;
}

/**
* Solves a 1x1 or 2x2 linear system with scaling to prevent overflow
*/
declare var dlaln2: Routine;

export = dlaln2;
