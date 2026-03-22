

// TypeScript declarations for @stdlib/lapack/base/dlarfb

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a real block Householder reflector to a matrix.
	*/
	(
		side: string,
		trans: string,
		direct: string,
		storev: string,
		M: number,
		N: number,
		K: number,
		V: Float64Array,
		strideV1: number,
		strideV2: number,
		offsetV: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK1: number,
		strideWORK2: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Apply a real block Householder reflector to a matrix.
*/
declare var dlarfb: Routine;

export = dlarfb;
