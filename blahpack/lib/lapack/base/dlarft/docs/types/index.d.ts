

// TypeScript declarations for @stdlib/lapack/base/dlarft

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Form the triangular factor T of a real block reflector.
	*/
	(
		direct: string,
		storev: string,
		N: number,
		K: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number
	): Float64Array;
}

/**
* Form the triangular factor T of a real block reflector.
*/
declare var dlarft: Routine;

export = dlarft;
