
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgbbrd = require( './dgbbrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgbbrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgbbrd;
