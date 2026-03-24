

// TypeScript declarations for @stdlib/lapack/base/dgebak

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Back-transforms eigenvectors after balancing by dgebal
	*/
	(
		job: string,
		side: string,
		N: number,
		ilo: number,
		ihi: number,
		SCALE: Float64Array,
		strideSCALE: number,
		offsetSCALE: number,
		M: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number
	): Float64Array;
}

/**
* Back-transforms eigenvectors after balancing by dgebal
*/
declare var dgebak: Routine;

export = dgebak;
