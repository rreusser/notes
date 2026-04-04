
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlacon = require( './zlacon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlacon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlacon;
