

// TypeScript declarations for @stdlib/lapack/base/dopgtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generates a real orthogonal matrix Q from the elementary reflectors returned by dsptrd.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Generates a real orthogonal matrix Q from the elementary reflectors returned by dsptrd.
*/
declare var dopgtr: Routine;

export = dopgtr;
