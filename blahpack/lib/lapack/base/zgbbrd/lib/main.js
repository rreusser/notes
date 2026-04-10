
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgbbrd = require( './zgbbrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgbbrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgbbrd;
