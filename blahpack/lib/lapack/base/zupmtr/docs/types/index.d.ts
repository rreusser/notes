

// TypeScript declarations for @stdlib/lapack/base/zupmtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Overwrites a general complex matrix with a transformation from the unitary matrix Q returned by zhptrd.
	*/
	(
		side: string,
		uplo: string,
		trans: string,
		M: number,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
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
* Overwrites a general complex matrix with a transformation from the unitary matrix Q returned by zhptrd.
*/
declare var zupmtr: Routine;

export = zupmtr;
