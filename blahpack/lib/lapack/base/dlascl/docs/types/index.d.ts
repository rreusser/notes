

// TypeScript declarations for @stdlib/lapack/base/dlascl

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Scale a matrix by CTO/CFROM with overflow protection
	*/
	(
		type: string,
		kl: number,
		ku: number,
		cfrom: number,
		cto: number,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Scale a matrix by CTO/CFROM with overflow protection
*/
declare var dlascl: Routine;

export = dlascl;
