'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgerqf = require( './dgerqf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgerqf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgerqf;
