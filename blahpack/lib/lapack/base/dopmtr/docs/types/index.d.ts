

// TypeScript declarations for @stdlib/lapack/base/dopmtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Overwrites a general matrix with a transformation from the orthogonal matrix Q returned by dsptrd.
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
* Overwrites a general matrix with a transformation from the orthogonal matrix Q returned by dsptrd.
*/
declare var dopmtr: Routine;

export = dopmtr;
