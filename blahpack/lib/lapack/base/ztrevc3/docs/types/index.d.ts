

// TypeScript declarations for @stdlib/lapack/base/ztrevc3

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes eigenvectors of a complex upper triangular matrix
	*/
	(
		side: string,
		howmny: string,
		SELECT: Float64Array,
		strideSELECT: number,
		offsetSELECT: number,
		N: number,
		T: Float64Array,
		strideT1: number,
		strideT2: number,
		offsetT: number,
		VL: Float64Array,
		strideVL1: number,
		strideVL2: number,
		offsetVL: number,
		VR: Float64Array,
		strideVR1: number,
		strideVR2: number,
		offsetVR: number,
		mm: number,
		M: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number,
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number,
		lrwork: number
	): Float64Array;
}

/**
* Computes eigenvectors of a complex upper triangular matrix
*/
declare var ztrevc3: Routine;

export = ztrevc3;
