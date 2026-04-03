

// TypeScript declarations for @stdlib/lapack/base/zlarfx

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Applies an elementary reflector to a general rectangular matrix with loop unrolling when the reflector has order at most 10.
	*/
	(
		side: string,
		M: number,
		N: number,
		v: Float64Array,
		strideV: number,
		offsetV: number,
		tau: any,
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
* Applies an elementary reflector to a general rectangular matrix with loop unrolling when the reflector has order at most 10.
*/
declare var zlarfx: Routine;

export = zlarfx;
