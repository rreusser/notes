
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var drotg = require( './drotg.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( drotg, 'ndarray', ndarray );


// EXPORTS //

module.exports = drotg;
