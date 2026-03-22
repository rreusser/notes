

// TypeScript declarations for @stdlib/lapack/base/dlarf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a real Householder reflector to a matrix.
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
* Apply a real Householder reflector to a matrix.
*/
declare var dlarf: Routine;

export = dlarf;
