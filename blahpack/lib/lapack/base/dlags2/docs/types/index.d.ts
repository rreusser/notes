

// TypeScript declarations for @stdlib/lapack/base/dlags2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes 2-by-2 orthogonal matrices U, V, Q for simultaneous upper/lower triangularization
	*/
	(
		upper: boolean,
		a1: number,
		a2: number,
		a3: number,
		b1: number,
		b2: number,
		b3: number,
		csu: number,
		snu: number,
		csv: number,
		snv: number,
		csq: number,
		snq: number
	): void;
}

/**
* Computes 2-by-2 orthogonal matrices U, V, Q for simultaneous upper/lower triangularization
*/
declare var dlags2: Routine;

export = dlags2;
