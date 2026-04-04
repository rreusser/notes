

// TypeScript declarations for @stdlib/lapack/base/dlaqgb

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Equilibrates a real general band matrix using row and column scaling factors.
	*/
	(
		M: number,
		N: number,
		kl: number,
		ku: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		r: Float64Array,
		strideR: number,
		offsetR: number,
		c: Float64Array,
		strideC: number,
		offsetC: number,
		rowcnd: number,
		colcnd: number,
		amax: number,
		equed: string
	): Float64Array;
}

/**
* Equilibrates a real general band matrix using row and column scaling factors.
*/
declare var dlaqgb: Routine;

export = dlaqgb;
