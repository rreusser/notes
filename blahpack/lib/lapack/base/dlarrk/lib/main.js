

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarrk = require( './dlarrk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarrk, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarrk;
