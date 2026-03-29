

// TypeScript declarations for @stdlib/lapack/base/dlasdt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Create a tree of subproblems for bidiagonal divide and conquer.
	*/
	(
		N: number,
		lvl: number,
		nd: number,
		INODE: Int32Array,
		strideINODE: number,
		offsetINODE: number,
		NDIML: Int32Array,
		strideNDIML: number,
		offsetNDIML: number,
		NDIMR: Int32Array,
		strideNDIMR: number,
		offsetNDIMR: number,
		msub: number
	): void;
}

/**
* Create a tree of subproblems for bidiagonal divide and conquer.
*/
declare var dlasdt: Routine;

export = dlasdt;
