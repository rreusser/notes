
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlacon = require( './dlacon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlacon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlacon;
