'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dggsvp3 = require( './dggsvp3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dggsvp3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dggsvp3;
