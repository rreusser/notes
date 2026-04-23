
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlaic1 = require( './zlaic1.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlaic1, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlaic1;
