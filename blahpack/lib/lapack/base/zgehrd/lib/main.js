

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgehrd = require( './zgehrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgehrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgehrd;
