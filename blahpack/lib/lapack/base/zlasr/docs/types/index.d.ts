

// TypeScript declarations for @stdlib/lapack/base/zlasr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a sequence of plane rotations to a complex matrix
	*/
	(
		side: string,
		pivot: string,
		direct: string,
		M: number,
		N: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Apply a sequence of plane rotations to a complex matrix
*/
declare var zlasr: Routine;

export = zlasr;
