

// TypeScript declarations for @stdlib/lapack/base/dlarfx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Applies an elementary reflector to a general matrix with unrolled loops
	*/
	(
		side: string,
		M: number,
		N: number,
		v: Float64Array,
		strideV: number,
		offsetV: number,
		tau: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Applies an elementary reflector to a general matrix with unrolled loops
*/
declare var dlarfx: Routine;

export = dlarfx;
