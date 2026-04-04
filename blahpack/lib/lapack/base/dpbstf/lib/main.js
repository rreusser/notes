
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpbstf = require( './dpbstf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbstf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbstf;
