

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dorgtr = require( './dorgtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgtr;
